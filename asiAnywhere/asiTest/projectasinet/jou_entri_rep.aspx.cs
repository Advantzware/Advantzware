
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.IO;
using System.Web.UI;
#endregion

public partial class jou_entri_rep : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "jou_entri_rep.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {
            }
        }
        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'jou_entri_rep.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begjoTextBox.Text = dr["field1"].ToString();
                    endjoTextBox.Text = dr["field2"].ToString();
                    begdateTextBox.Text = dr["field3"].ToString();
                    enddateTextBox.Text = dr["field4"].ToString();

                    if (dr["rd_field1"].ToString() == "Account#")
                        RadioButtonList1.SelectedIndex = 0;
                    else
                        RadioButtonList1.SelectedIndex = 1;

                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }

            if (begdateTextBox.Text == "")
                begdateTextBox.Text = System.DateTime.Today.ToShortDateString();
            if (enddateTextBox.Text == "")
                enddateTextBox.Text = System.DateTime.Today.ToShortDateString();
            if (endjoTextBox.Text == "")
            {
                endjoTextBox.Text = "99999999";
            }

            RadioButtonList3.SelectedIndex = 0;

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
        }

        if (Session["User"] != null)
        {
            lblUser.Text = UserLogin.UserName;
        }


    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        if (RadioButtonList1.SelectedIndex == 0)
        {
            HiddenField1.Value = "Account#";
        }
        else
        {
            HiddenField1.Value = "Sequence";
        }



        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "journal";
        ObjectDataSource1.SelectParameters["prmBegJur"].DefaultValue = begjoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegDate"].DefaultValue = begdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndJur"].DefaultValue = endjoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = enddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;
        
        



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'jou_entri_rep.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4,rd_field1) values ('" + UserLogin.UserName + "','jou_entri_rep.aspx' , '" + begjoTextBox.Text.Trim() + "', '" + endjoTextBox.Text.Trim() + "','" + begdateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "','" + Convert.ToString(HiddenField1.Value) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begjoTextBox.Text.Trim() + "', field2 = '" + endjoTextBox.Text.Trim() + "', field3 = '" + begdateTextBox.Text.Trim() + "', field4 = '" + enddateTextBox.Text.Trim() + "',chk_field1 = '" + Convert.ToString(HiddenField1.Value) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'jou_entri_rep.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {

            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("gljfileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                //Response.Write("<script>window.location.href='jou_entri_rep.aspx'</script>");
            }
        }
        catch { }


    }

       

}

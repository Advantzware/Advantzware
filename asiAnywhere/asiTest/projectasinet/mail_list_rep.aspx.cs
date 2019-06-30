
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

public partial class mail_list_rep : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "mail_list_rep.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'mail_list_rep.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begstatTextBox.Text = dr["field1"].ToString();
                    endstatTextBox.Text = dr["field2"].ToString();
                    begvendTextBox.Text = dr["field3"].ToString();
                    endvendTextBox.Text = dr["field4"].ToString();
                    begbuyerTextBox.Text = dr["field5"].ToString();
                    endbuyerTextBox.Text = dr["field6"].ToString();


                    if (dr["rd_field1"].ToString() == "Active")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Inactive")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "All Vendors")
                        RadioButtonList1.SelectedIndex = 2;


                    if (dr["rd_field2"].ToString() == "Labels")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "ASCII")
                        RadioButtonList2.SelectedIndex = 1;
                                       
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

            if (endstatTextBox.Text == "")
                endstatTextBox.Text = "zz";
            if (endvendTextBox.Text == "")
                endvendTextBox.Text = "zzzzzzzz";
            if (endbuyerTextBox.Text == "")
                endbuyerTextBox.Text = "zzz";
                        
            if (RadioButtonList1.SelectedIndex == -1)
            {
                RadioButtonList1.SelectedIndex = 0;
            }
            if (RadioButtonList2.SelectedIndex == -1)
            {
                RadioButtonList2.SelectedIndex = 0;
            }
            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
            RadioButtonList3.SelectedIndex = 0;
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


        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmmail"].DefaultValue = "mail";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;
        ObjectDataSource1.SelectParameters["prmbegstat"].DefaultValue = begstatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendstat"].DefaultValue = endstatTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegtyp"].DefaultValue = begvendTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmendtyp"].DefaultValue = endvendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegbuy"].DefaultValue = begbuyerTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendbuy"].DefaultValue = endbuyerTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmactiv"].DefaultValue = RadioButtonList1.SelectedValue;


        ObjectDataSource1.SelectParameters["prmoutput"].DefaultValue = RadioButtonList2.SelectedValue;
        
        



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'mail_list_rep.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, rd_field1, rd_field2) values ('" + UserLogin.UserName + "','mail_list_rep.aspx' , '" + begstatTextBox.Text.Trim() + "', '" + endstatTextBox.Text.Trim() + "','" + begvendTextBox.Text.Trim() + "','" + endvendTextBox.Text.Trim() + "','" + begbuyerTextBox.Text.Trim() + "','" + endbuyerTextBox.Text.Trim() + "','" + RadioButtonList1.SelectedValue + "','" + RadioButtonList2.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begstatTextBox.Text.Trim() + "', field2 = '" + endstatTextBox.Text.Trim() + "', field3 = '" + begvendTextBox.Text.Trim() + "', field4 = '" + endvendTextBox.Text.Trim() + "', field5 = '" + begbuyerTextBox.Text.Trim() + "', field6 = '" + endbuyerTextBox.Text.Trim() + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "', rd_field2 = '" + RadioButtonList2.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'mail_list_rep.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("mailLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='mail_list_rep.aspx'</script>");
            }
        }
        catch { }


    }

       

}

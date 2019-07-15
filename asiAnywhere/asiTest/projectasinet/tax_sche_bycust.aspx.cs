
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

public partial class tax_sche_bycust : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "tax_sche_bycust.aspx";
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

            try
            {
                string UserId = UserLogin.UserName;
                string aDefaultCust = null;
                string aComp = null;

               // func1 user = new func1();
                //user.CheckUserCustomer(aComp, UserId, ref  aDefaultCust);
                
            }
            catch { }

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'tax_sche_bycust.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    foryearTextBox.Text = dr["field1"].ToString();
                    perTextBox.Text = dr["field2"].ToString();
                    begdateTextBox.Text = dr["field3"].ToString();
                    enddateTextBox.Text = dr["field4"].ToString();
                                                           
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


        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmtaxcust"].DefaultValue = "taxcust";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;
        ObjectDataSource1.SelectParameters["prmbegdt"].DefaultValue = begdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmenddt"].DefaultValue = enddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegyr"].DefaultValue = foryearTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmperiod"].DefaultValue = perTextBox.Text.Trim();
        



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'tax_sche_bycust.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4) values ('" + UserLogin.UserName + "','tax_sche_bycust.aspx' , '" + foryearTextBox.Text.Trim() + "', '" + perTextBox.Text.Trim() + "','" + begdateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + foryearTextBox.Text.Trim() + "', field2 = '" + perTextBox.Text.Trim() + "', field3 = '" + begdateTextBox.Text.Trim() + "', field4 = '" + enddateTextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'tax_sche_bycust.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("taxcustLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='tax_sche_bycust.aspx'</script>");
            }
        }
        catch { }


    }

       

}

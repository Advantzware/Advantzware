
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

public partial class gl_auto_dist : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "gl_auto_dist.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'gl_auto_dist.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    begdateTextBox.Text = dr["field3"].ToString();
                    enddateTextBox.Text = dr["field4"].ToString();
                    begactTextBox.Text = dr["field1"].ToString();
                    endactTextBox.Text = dr["field2"].ToString();
                    transTextBox.Text = dr["field5"].ToString();
                    perTextBox.Text = dr["field6"].ToString();



                    if (dr["rd_field1"].ToString() == "D")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "S")
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


            if (endactTextBox.Text == "")
            {
                endactTextBox.Text = "zzzzzzzzzzzzzzzzzzzzzzzzzz";
            }

            transTextBox.Text = System.DateTime.Today.ToShortDateString();
            perTextBox.Text = System.DateTime.Today.Month.ToString();

            transTextBox.Text = System.DateTime.Now.Date.ToShortDateString();

            if (RadioButtonList1.SelectedIndex == -1)
            {
                RadioButtonList1.SelectedIndex = 0;
            }
            
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
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "journal";
        ObjectDataSource1.SelectParameters["prmBegacc"].DefaultValue = begactTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndacc"].DefaultValue = endactTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegindate"].DefaultValue = begdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEnddate"].DefaultValue = enddateTextBox.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;
        ObjectDataSource1.SelectParameters["prmReport"].DefaultValue = RadioButtonList1.SelectedValue;
        ObjectDataSource1.SelectParameters["prmPost"].DefaultValue = HiddenFieldPost.Value;
        ObjectDataSource1.SelectParameters["prmTrnsDate"].DefaultValue = transTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPeriod"].DefaultValue = perTextBox.Text.Trim();

        
        



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'gl_auto_dist.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);
            //Response.Write(ds.Tables[0].TableName);
            //Response.Write(ds.Tables[0].Rows.Count);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, rd_field1) values ('" + UserLogin.UserName + "','gl_auto_dist.aspx' , '" + begactTextBox.Text.Trim() + "', '" + endactTextBox.Text.Trim() + "','" + begdateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "','" + transTextBox.Text.Trim() + "','" + perTextBox.Text.Trim() + "','" + RadioButtonList1.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + begactTextBox.Text.Trim() + "', field2 = '" + endactTextBox.Text.Trim() + "', field3 = '" + begdateTextBox.Text.Trim() + "', field4 = '" + enddateTextBox.Text.Trim() + "', field5 = '" + transTextBox.Text.Trim() + "', field6 = '" + perTextBox.Text.Trim() + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'gl_auto_dist.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("distpostLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='gl_auto_dist.aspx'</script>");
            }
        }
        catch { }


    }

       

}

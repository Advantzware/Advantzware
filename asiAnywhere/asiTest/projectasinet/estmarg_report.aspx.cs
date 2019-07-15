#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class estmarg_report: System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "estmarg_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {
                Image4.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            HyperLink1.Visible = false;
            OutPutFile.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'estmarg_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    BeCustTextBox.Text  = dr["field1"].ToString();
                    EndcustTextBox.Text = dr["field2"].ToString();
                    BeSmanTextBox.Text  = dr["field3"].ToString();
                    EndSmanTextBox.Text = dr["field4"].ToString();
                    BeEstTextBox.Text  = dr["field5"].ToString();
                    EndEstTextBox.Text = dr["field6"].ToString();
                    BeAddTextBox.Text  = dr["field7"].ToString();
                    EndAddTextBox.Text = dr["field8"].ToString();
                    BeModTextBox.Text  = dr["field9"].ToString();
                    EndModTextBox.Text = dr["field10"].ToString();                  

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

            try
            {
                if (BeCustTextBox.Text == "")
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    BeCustTextBox.Text = begin.Text;
                    EndcustTextBox.Text = begin.Text;
                }
                
                if (BeAddTextBox.Text == "")
                    BeAddTextBox.Text = "01/01/0001";
                if (EndAddTextBox.Text == "")
                    EndAddTextBox.Text = "12/31/2099";
                if (BeModTextBox.Text == "")
                    BeModTextBox.Text = "01/01/0001";
                if (EndModTextBox.Text == "")
                    EndModTextBox.Text = "12/31/2099";
                if (EndSmanTextBox.Text == "")
                    EndSmanTextBox.Text = "zzz";
                if (EndEstTextBox.Text == "")
                    EndEstTextBox.Text = "99999999";
                              

                RadioButtonList_out.SelectedIndex = 0;
            }
            catch { }                   
            
            if (Session["User"] != null)
            {               
                lblUser.Text = UserLogin.UserName;
            }
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "magan";
        ObjectDataSource1.SelectParameters["prmBegCust"].DefaultValue = BeCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = EndcustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegSman"].DefaultValue = BeSmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSman"].DefaultValue = EndSmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegEst"].DefaultValue = BeEstTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndEst"].DefaultValue = EndEstTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAddDate"].DefaultValue = BeAddTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmEndAddDate"].DefaultValue = EndAddTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmModDate"].DefaultValue = BeModTextBox.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmEndModDate"].DefaultValue = EndModTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;
        
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'estmarg_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8,field9, field10) values ('" + UserLogin.UserName + "','estmarg_report.aspx' , '" + BeCustTextBox.Text.Trim() + "', '" + EndcustTextBox.Text.Trim() + "','" + BeSmanTextBox.Text.Trim() + "','" + EndSmanTextBox.Text.Trim() + "','" + BeEstTextBox.Text.Trim() + "','" + EndEstTextBox.Text.Trim() + "','" + BeAddTextBox.Text.Trim() + "','" + EndAddTextBox.Text.Trim() + "','" + BeModTextBox.Text.Trim() + "','" + EndModTextBox.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BeCustTextBox.Text.Trim() + "', field2 = '" + EndcustTextBox.Text.Trim() + "', field3 = '" + BeSmanTextBox.Text.Trim() + "', field4 = '" + EndSmanTextBox.Text.Trim() + "', field5 = '" + BeEstTextBox.Text.Trim() + "', field6 = '" + EndEstTextBox.Text.Trim() + "', field7 = '" + BeAddTextBox.Text.Trim() + "', field8 = '" + EndAddTextBox.Text.Trim() + "', field9 = '" + BeModTextBox.Text.Trim() + "', field10 = '" + EndModTextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'estmarg_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("wmarginsfileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;
            

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                
            }
        }
            catch{}
        
    }
    
   
}

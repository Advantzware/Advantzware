using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET1;
using ASIDataNS;
using Progress.Open4GL.Proxy;
using System.Data.SqlClient;

/// <summary>
/// Summary description for Class1
/// </summary>

public partial class download_cust : System.Web.UI.Page
{
    Int64[] rec;
    int i = 0;

    protected void Page_Load(object sender, System.EventArgs e)
    {
        Session["contact_rec_key2"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "download_customer_notes.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            Session["dc_user_comp"] = lblComp.Text;
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        Session["Rowuser"] = UserLogin.UserName;
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
                Session["dc_user_login"] = lblUser.Text;
            }
        }
        GridView1.SelectedIndex = Convert.ToInt32(Session["down_dc_index"]);
        if (Session["down_dc_index"] == null)
        {
            GridView1.SelectedIndex = 0;
            foreach (GridViewRow gv in GridView1.Rows)
            {

                //Session["dc_note_date"] = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
                //Session["dc_note_time"] = ((Label)GridView1.SelectedRow.FindControl("Label4")).Text;
                //Session["dc_note_title"] = ((Label)GridView1.SelectedRow.FindControl("Label5")).Text;
                //Session["dc_user_id"] = ((Label)GridView1.SelectedRow.FindControl("Label6")).Text;

                //Session["dc_note_text"] = ((Label)GridView1.SelectedRow.FindControl("Label7")).Text;
                //Session["dc_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("Label8")).Text;

            }
        }
        DataSet old = new DataSet();
        download dd = new download();
        old = dd.custnotes("select", Convert.ToString(Session["dc_user_login"]), Convert.ToString(Session["dc_user_comp"]), txt_custno.Text.Trim());

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());

        try
        {
            string cmd = "select * from notes";

            SqlDataAdapter danew = new SqlDataAdapter(cmd, conn);
            DataSet dsOld = new DataSet();
            DataSet dsNew = new DataSet();
            dsOld = old.Copy();
            danew.Fill(dsNew);

            DataTable dtOld = dsOld.Tables[0];
            DataTable dtNew = dsNew.Tables[0];
            Int64 n = dtNew.Rows.Count;
            rec = new Int64[n];
            for (int i = 0; i < dtNew.Rows.Count; i++)
            {
                rec[i] = Convert.ToInt64(dtNew.Rows[i]["rec_key"]);
            }

        }

        catch
        {
            return;
        }
        finally
        {
            conn.Close();
        }
        try
        {
            CheckBox view = (CheckBox)GridView1.FindControl("CheckBox1");
            if (view.Checked)
            {
                Session["dc_checked"] = "True";
            }
            else
            {
                Session["dc_checked"] = "False";
            }
        }
        catch
        {
            return;
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
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

    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }


        Response.Redirect(sMenuURL);
    }


    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["down_dc_index"] = GridView1.SelectedIndex;

        foreach (GridViewRow gv in GridView1.Rows)
        {
            //Session["dc_cust_no"] = ((Label)GridView1.SelectedRow.FindControl("Label1")).Text;
            //Session["dc_contact"] = ((Label)GridView1.SelectedRow.FindControl("Label2")).Text;
            //Session["dc_note_date"] = ((Label)GridView1.SelectedRow.FindControl("Label3")).Text;
            //Session["dc_note_time"] = ((Label)GridView1.SelectedRow.FindControl("Label4")).Text;
            //Session["dc_note_title"] = ((Label)GridView1.SelectedRow.FindControl("Label5")).Text;
            //Session["dc_user_id"] = ((Label)GridView1.SelectedRow.FindControl("Label6")).Text;

            //Session["dc_note_text"] = ((Label)GridView1.SelectedRow.FindControl("Label7")).Text;
            //Session["dc_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("Label8")).Text;
            //CheckBox view = (CheckBox)GridView1.SelectedRow.FindControl("CheckBox1");
            //if (view.Checked)
            //{
            //    Session["dc_checked"] = "True";
            //}
            //else
            //{
            //    Session["dc_checked"] = "False";
            //}           
        }

    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        //int j = rec.Length-1;

        //for (int i = 0; i < rec.Length; i++)
        //{
        //    if (Convert.ToInt64(Session["dc_rec_key"]) == rec[i])
        //    {
        //        Response.Write("<script>alert('This Record Allready Exists')</script>");
        //        break;
        //    }

        //    if (j == i)
        //    {
        //        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        //        try
        //        {
        //            conn.Open();
        //            SqlCommand cmd = new SqlCommand("insert into notes (rec_key,note_date,note_time,user_id,note_title,note_text,viewed) values ('" + Session["dc_rec_key"] + "','" + Session["dc_note_date"] + "','" + Session["dc_note_time"] + "','" + Session["dc_user_id"] + "','" + Session["dc_note_title"] + "','" + Session["dc_note_text"] + "','" + Session["dc_checked"] + "')", conn);
        //            cmd.ExecuteNonQuery();
        //            conn.Close();
        //        }
        //        catch
        //        {
        //            return;
        //        }
        //        finally
        //        {
        //            conn.Close();
        //        }
        //    }  
        //}  

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        for (int i = 0; i < GridView1.Rows.Count; i++)
        {
            GridViewRow row = GridView1.Rows[i];
            bool ischeck = ((CheckBox)row.FindControl("chk1")).Checked;
            if (ischeck)
            {
                try
                {
                    conn.Open();

                    //Response.Write(((Label)row.FindControl("Label1")).Text);
                    //Response.Write(((Label)row.FindControl("Label2")).Text);
                    //Response.Write(((Label)row.FindControl("Label3")).Text);
                    //Response.Write(((Label)row.FindControl("Label4")).Text);

                    //Response.Write(((Label)row.FindControl("Label5")).Text);
                    //Response.Write(((Label)row.FindControl("Label6")).Text);
                    //Response.Write(((Label)row.FindControl("Label7")).Text);

                    //Response.Write(((Label)row.FindControl("Label8")).Text);
                    //Response.Write(((CheckBox)row.FindControl("CheckBox1")).Checked);

                    SqlCommand cmd = new SqlCommand("insert into notes (rec_key,d_rec_key,note_date,note_time,user_id,note_title,note_text,viewed) values ('" + ((Label)row.FindControl("Label8")).Text + "','" + ((Label)row.FindControl("Label8")).Text + "','" + ((Label)row.FindControl("Label3")).Text + "','" + ((Label)row.FindControl("Label4")).Text + "','" + ((Label)row.FindControl("Label6")).Text + "','" + ((Label)row.FindControl("Label5")).Text + "','" + ((Label)row.FindControl("Label7")).Text + "','" + ((CheckBox)row.FindControl("CheckBox1")).Text + "')", conn);
                    //Response.Write(((Label)row.FindControl("Label8")).Text);
                    cmd.ExecuteNonQuery();
                    conn.Close();
                }
                catch
                {
                    Response.Write("");
                }
                finally
                {
                    conn.Close();
                }
            }
        }

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_custno.Text.Trim();
        Page_Load(sender, e);
    }
    protected void btnAll_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
        txt_custno.Text = "";
    }
    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }

}